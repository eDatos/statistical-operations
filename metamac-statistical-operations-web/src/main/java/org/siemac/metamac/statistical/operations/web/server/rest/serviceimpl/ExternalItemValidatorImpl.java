package org.siemac.metamac.statistical.operations.web.server.rest.serviceimpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.web.server.rest.SrmRestInternalFacade;
import org.siemac.metamac.statistical.operations.web.server.rest.serviceapi.ExternalItemValidator;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeRestCriteria;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.criteria.SrmExternalResourceRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service("externalItemValidator")
public class ExternalItemValidatorImpl implements ExternalItemValidator {

    @Autowired
    private SrmRestInternalFacade srmRestInternalFacade;

    /**
     * Check that an {@link ExternalItemDto} is externally published
     */
    @Override
    public void checkExternalItemIsExternallyPublished(ServiceContext serviceContext, String externalItemName, ExternalItemDto externalItemDto, MetamacWebException metamacWebException)
            throws MetamacWebException {
        if (externalItemDto != null) {
            Set<ExternalItemDto> externalItemDtos = new HashSet<ExternalItemDto>();
            externalItemDtos.add(externalItemDto);
            checkExternalItemsAreExternallyPublished(serviceContext, externalItemName, externalItemDtos, metamacWebException);
        }
    };

    /**
     * Check that the {@link ExternalItemDto}s are externally published
     */
    @Override
    public void checkExternalItemsAreExternallyPublished(ServiceContext serviceContext, String externalItemName, Set<ExternalItemDto> externalItemDtos, MetamacWebException metamacWebException)
            throws MetamacWebException {

        try {

            if (externalItemDtos != null) {

                Map<TypeExternalArtefactsEnum, List<ExternalItemDto>> externalItemsByType = new HashMap<TypeExternalArtefactsEnum, List<ExternalItemDto>>();

                for (ExternalItemDto externalItemDto : externalItemDtos) {
                    TypeExternalArtefactsEnum type = externalItemDto.getType();
                    List<ExternalItemDto> items = externalItemsByType.get(type);
                    if (items == null) {
                        items = new ArrayList<ExternalItemDto>();
                        externalItemsByType.put(type, items);
                    }
                    items.add(externalItemDto);
                }

                checkCategoriesAreExternallyPublished(serviceContext, externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CATEGORY));
                checkConceptsAreExternallyPublished(serviceContext, externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CONCEPT));
                checkCodesAreExternallyPublished(serviceContext, externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CODE));
                checkCodeListsAreExternallyPublished(serviceContext, externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CODELIST));
                checkConceptSchemesAreExternallyPublished(serviceContext, externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CONCEPT_SCHEME));
                checkDataProvidersAreExternallyPublished(serviceContext, externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.DATA_PROVIDER));
                checkOrganizationUnitsAreExternallyPublished(serviceContext, externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.ORGANISATION_UNIT));

                if (!externalItemsByType.keySet().isEmpty()) {
                    throwExternalItemNotFoundException(serviceContext, externalItemName);
                }
            }

        } catch (MetamacWebException e) {
            metamacWebException.getWebExceptionItems().addAll(e.getWebExceptionItems());
        }
    }

    //
    // CATEGORIES
    //

    private void checkCategoriesAreExternallyPublished(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemRestCriteria criteria = buildItemCriteriaToCheckExternalPublication(serviceContext, externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findCategories(serviceContext, criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(serviceContext, externalItemName, result, externalItemDtos);
        }
    }

    //
    // CONCEPTS
    //

    private void checkConceptsAreExternallyPublished(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            ConceptRestCriteria criteria = buildConceptCriteriaToCheckExternalPublication(serviceContext, externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findConcepts(serviceContext, criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(serviceContext, externalItemName, result, externalItemDtos);
        }
    }

    //
    // CODES
    //

    private void checkCodesAreExternallyPublished(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemRestCriteria criteria = buildItemCriteriaToCheckExternalPublication(serviceContext, externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findCodes(serviceContext, criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(serviceContext, externalItemName, result, externalItemDtos);
        }
    }

    //
    // CODELISTS
    //

    private void checkCodeListsAreExternallyPublished(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmExternalResourceRestCriteria criteria = buildItemSchemeCriteriaToCheckExternalPublication(serviceContext, externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findCodelists(serviceContext, criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(serviceContext, externalItemName, result, externalItemDtos);
        }
    }

    //
    // CONCEPT SCHEMES
    //

    private void checkConceptSchemesAreExternallyPublished(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            ConceptSchemeRestCriteria criteria = buildConceptSchemeCriteriaToCheckExternalPublication(serviceContext, externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findConceptSchemes(serviceContext, criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(serviceContext, externalItemName, result, externalItemDtos);
        }
    }

    //
    // DATA PROVIDERS
    //

    private void checkDataProvidersAreExternallyPublished(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemRestCriteria criteria = buildItemCriteriaToCheckExternalPublication(serviceContext, externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findDataProviders(serviceContext, criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(serviceContext, externalItemName, result, externalItemDtos);
        }
    }

    //
    // ORGANIZATION UNITS
    //

    private void checkOrganizationUnitsAreExternallyPublished(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemRestCriteria criteria = buildItemCriteriaToCheckExternalPublication(serviceContext, externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findOrganisationUnits(serviceContext, criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(serviceContext, externalItemName, result, externalItemDtos);
        }
    }

    private ConceptRestCriteria buildConceptCriteriaToCheckExternalPublication(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos)
            throws MetamacWebException {
        return (ConceptRestCriteria) buildItemCriteriaToCheckExternalPublication(serviceContext, new ConceptRestCriteria(), externalItemName, externalItemDtos);
    }

    private ConceptSchemeRestCriteria buildConceptSchemeCriteriaToCheckExternalPublication(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos)
            throws MetamacWebException {
        return (ConceptSchemeRestCriteria) buildItemSchemeCriteriaToCheckExternalPublication(serviceContext, new ConceptSchemeRestCriteria(), externalItemName, externalItemDtos);
    }

    //
    // UTILITY METHODS
    //

    private SrmExternalResourceRestCriteria buildItemSchemeCriteriaToCheckExternalPublication(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos)
            throws MetamacWebException {
        return buildItemSchemeCriteriaToCheckExternalPublication(serviceContext, new SrmExternalResourceRestCriteria(), externalItemName, externalItemDtos);
    }

    private SrmExternalResourceRestCriteria buildItemSchemeCriteriaToCheckExternalPublication(ServiceContext serviceContext, SrmExternalResourceRestCriteria criteria, String externalItemName,
            List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        criteria.setOnlyExternallyPublished(Boolean.TRUE);
        criteria.setUrns(new ArrayList<String>());
        for (ExternalItemDto externalItemDto : externalItemDtos) {
            criteria.getUrns().add(getExternalItemUrn(serviceContext, externalItemName, externalItemDto));
        }
        return criteria;
    }

    private SrmItemRestCriteria buildItemCriteriaToCheckExternalPublication(ServiceContext serviceContext, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        return buildItemCriteriaToCheckExternalPublication(serviceContext, new SrmItemRestCriteria(), externalItemName, externalItemDtos);
    }

    private SrmItemRestCriteria buildItemCriteriaToCheckExternalPublication(ServiceContext serviceContext, SrmItemRestCriteria criteria, String externalItemName, List<ExternalItemDto> externalItemDtos)
            throws MetamacWebException {
        criteria.setItemSchemeExternallyPublished(Boolean.TRUE);
        criteria.setUrns(new ArrayList<String>());
        for (ExternalItemDto externalItemDto : externalItemDtos) {
            criteria.getUrns().add(getExternalItemUrn(serviceContext, externalItemName, externalItemDto));
        }
        return criteria;
    }

    private void validateExternalItemsResult(ServiceContext serviceContext, String externalItemName, ExternalItemsResult result, List<ExternalItemDto> expectedExternalItems)
            throws MetamacWebException {
        for (ExternalItemDto expected : expectedExternalItems) {
            if (!containsExternalItem(expected, result.getExternalItemDtos())) {
                throwExternalItemNotExternallyPublishedException(serviceContext, externalItemName);
            }
        }
    }

    private boolean containsExternalItem(ExternalItemDto expectedExternalItem, List<ExternalItemDto> externalItemDtos) {
        for (ExternalItemDto ei : externalItemDtos) {
            if (StringUtils.equals(expectedExternalItem.getUrn(), ei.getUrn()) && expectedExternalItem.getUrn() != null) {
                return true;
            }
            if (StringUtils.equals(expectedExternalItem.getUrnProvider(), ei.getUrnProvider()) && expectedExternalItem.getUrnProvider() != null) {
                return true;
            }
        }
        return false;
    }

    private String getExternalItemUrn(ServiceContext serviceContext, String externalItemName, ExternalItemDto externalItemDto) throws MetamacWebException {
        String urn = externalItemDto.getUrn();
        String urnProvider = externalItemDto.getUrnProvider();
        if (StringUtils.isBlank(urn) && StringUtils.isBlank(urnProvider)) {
            // The URN or the internal URN must be filled
            throwExternalItemNotFoundException(serviceContext, externalItemName);
        }
        if (StringUtils.isNotBlank(urn)) {
            return urn;
        } else {
            return urnProvider;
        }
    }

    private void throwExternalItemNotFoundException(ServiceContext serviceContext, String externalItemName) throws MetamacWebException {
        WebExceptionUtils.throwMetamacWebException(serviceContext, ServiceExceptionType.EXTERNAL_ITEM_NOT_FOUND, externalItemName);
    }

    private void throwExternalItemNotExternallyPublishedException(ServiceContext serviceContext, String externalItemName) throws MetamacWebException {
        WebExceptionUtils.throwMetamacWebException(serviceContext, ServiceExceptionType.EXTERNAL_ITEM_NOT_PUBLISHED, externalItemName);
    }
}
