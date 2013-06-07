package org.siemac.metamac.statistical.operations.web.server.rest.serviceimpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.server.rest.SrmRestInternalFacade;
import org.siemac.metamac.statistical.operations.web.server.rest.serviceapi.ExternalItemValidator;
import org.siemac.metamac.statistical.operations.web.shared.constants.WebMessageExceptionsConstants;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemSchemeRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.siemac.metamac.web.common.shared.exception.MetamacWebExceptionItem;
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
    public void checkExternalItemIsExternallyPublished(String externalItemName, ExternalItemDto externalItemDto) throws MetamacWebException {
        if (externalItemDto != null) {
            Set<ExternalItemDto> externalItemDtos = new HashSet<ExternalItemDto>();
            externalItemDtos.add(externalItemDto);
            checkExternalItemsAreExternallyPublished(externalItemName, externalItemDtos);
        }
    };

    /**
     * Check that the {@link ExternalItemDto}s are externally published
     */
    @Override
    public void checkExternalItemsAreExternallyPublished(String externalItemName, Set<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null) {

            // Divide the external items in different lists, taking into account their type
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

            checkCategoriesAreExternallyPublished(externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CATEGORY));
            checkConceptsAreExternallyPublished(externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CONCEPT));
            checkCodesAreExternallyPublished(externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CODE));
            checkCodeListsAreExternallyPublished(externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CODELIST));
            checkConceptSchemesAreExternallyPublished(externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.CONCEPT_SCHEME));
            checkDataProvidersAreExternallyPublished(externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.DATA_PROVIDER));
            checkOrganizationUnitsAreExternallyPublished(externalItemName, externalItemsByType.remove(TypeExternalArtefactsEnum.ORGANISATION_UNIT));

            if (!externalItemsByType.keySet().isEmpty()) {
                throwExternalItemNotFoundException(externalItemName);
            }
        }
    }

    //
    // CATEGORIES
    //

    private void checkCategoriesAreExternallyPublished(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemRestCriteria criteria = buildItemCriteriaToCheckExternalPublication(externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findCategories(criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(externalItemName, result, externalItemDtos);
        }
    }

    //
    // CONCEPTS
    //

    private void checkConceptsAreExternallyPublished(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            ConceptRestCriteria criteria = buildConceptCriteriaToCheckExternalPublication(externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findConcepts(criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(externalItemName, result, externalItemDtos);
        }
    }

    //
    // CODES
    //

    private void checkCodesAreExternallyPublished(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemRestCriteria criteria = buildItemCriteriaToCheckExternalPublication(externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findCodes(criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(externalItemName, result, externalItemDtos);
        }
    }

    //
    // CODELISTS
    //

    private void checkCodeListsAreExternallyPublished(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemSchemeRestCriteria criteria = buildItemSchemeCriteriaToCheckExternalPublication(externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findCodelists(criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(externalItemName, result, externalItemDtos);
        }
    }

    //
    // CONCEPT SCHEMES
    //

    private void checkConceptSchemesAreExternallyPublished(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            ConceptSchemeRestCriteria criteria = buildConceptSchemeCriteriaToCheckExternalPublication(externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findConceptSchemes(criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(externalItemName, result, externalItemDtos);
        }
    }

    //
    // DATA PROVIDERS
    //

    private void checkDataProvidersAreExternallyPublished(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemRestCriteria criteria = buildItemCriteriaToCheckExternalPublication(externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findDataProviders(criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(externalItemName, result, externalItemDtos);
        }
    }

    //
    // ORGANIZATION UNITS
    //

    private void checkOrganizationUnitsAreExternallyPublished(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        if (externalItemDtos != null && !externalItemDtos.isEmpty()) {
            SrmItemRestCriteria criteria = buildItemCriteriaToCheckExternalPublication(externalItemName, externalItemDtos);
            ExternalItemsResult result = srmRestInternalFacade.findOrganisationUnits(criteria, 0, externalItemDtos.size());
            validateExternalItemsResult(externalItemName, result, externalItemDtos);
        }
    }

    private ConceptRestCriteria buildConceptCriteriaToCheckExternalPublication(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        return (ConceptRestCriteria) buildItemCriteriaToCheckExternalPublication(new ConceptRestCriteria(), externalItemName, externalItemDtos);
    }

    private ConceptSchemeRestCriteria buildConceptSchemeCriteriaToCheckExternalPublication(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        return (ConceptSchemeRestCriteria) buildItemSchemeCriteriaToCheckExternalPublication(new ConceptSchemeRestCriteria(), externalItemName, externalItemDtos);
    }

    //
    // UTILITY METHODS
    //

    private SrmItemSchemeRestCriteria buildItemSchemeCriteriaToCheckExternalPublication(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        return buildItemSchemeCriteriaToCheckExternalPublication(new SrmItemSchemeRestCriteria(), externalItemName, externalItemDtos);
    }

    private SrmItemSchemeRestCriteria buildItemSchemeCriteriaToCheckExternalPublication(SrmItemSchemeRestCriteria criteria, String externalItemName, List<ExternalItemDto> externalItemDtos)
            throws MetamacWebException {
        criteria.setIsExternallyPublished(Boolean.TRUE);
        criteria.setUrns(new ArrayList<String>());
        for (ExternalItemDto externalItemDto : externalItemDtos) {
            criteria.getUrns().add(getExternalItemUrn(externalItemName, externalItemDto));
        }
        return criteria;
    }

    private SrmItemRestCriteria buildItemCriteriaToCheckExternalPublication(String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        return buildItemCriteriaToCheckExternalPublication(new SrmItemRestCriteria(), externalItemName, externalItemDtos);
    }

    private SrmItemRestCriteria buildItemCriteriaToCheckExternalPublication(SrmItemRestCriteria criteria, String externalItemName, List<ExternalItemDto> externalItemDtos) throws MetamacWebException {
        criteria.setIsItemSchemeExternallyPublished(Boolean.TRUE);
        criteria.setUrns(new ArrayList<String>());
        for (ExternalItemDto externalItemDto : externalItemDtos) {
            criteria.getUrns().add(getExternalItemUrn(externalItemName, externalItemDto));
        }
        return criteria;
    }

    private void validateExternalItemsResult(String externalItemName, ExternalItemsResult result, List<ExternalItemDto> expectedExternalItems) throws MetamacWebException {
        for (ExternalItemDto expected : expectedExternalItems) {
            if (!containsExternalItem(expected, result.getExternalItemDtos())) {
                throwExternalItemNotExternallyPublishedException(externalItemName);
            }
        }
    }

    private boolean containsExternalItem(ExternalItemDto expectedExternalItem, List<ExternalItemDto> externalItemDtos) {
        for (ExternalItemDto ei : externalItemDtos) {
            if (StringUtils.equals(expectedExternalItem.getUrn(), ei.getUrn()) && expectedExternalItem.getUrn() != null) {
                return true;
            }
            if (StringUtils.equals(expectedExternalItem.getUrnInternal(), ei.getUrnInternal()) && expectedExternalItem.getUrnInternal() != null) {
                return true;
            }
        }
        return false;
    }

    private String getExternalItemUrn(String externalItemName, ExternalItemDto externalItemDto) throws MetamacWebException {
        String urn = externalItemDto.getUrn();
        String urnInternal = externalItemDto.getUrnInternal();
        if (StringUtils.isBlank(urn) && StringUtils.isBlank(urnInternal)) {
            // The URN or the internal URN must be filled
            throwExternalItemNotFoundException(externalItemName);
        }
        if (StringUtils.isNotBlank(urn)) {
            return urn;
        } else {
            return urnInternal;
        }
    }

    private void throwExternalItemNotFoundException(String externalItemName) throws MetamacWebException {
        MetamacWebExceptionItem exceptionItem = new MetamacWebExceptionItem(WebMessageExceptionsConstants.RESOURCE_ERROR_EXTERNAL_ITEM_NOT_FOUND, "The external item has not been found",
                externalItemName);
        MetamacWebException metamacWebException = new MetamacWebException();
        metamacWebException.getWebExceptionItems().add(exceptionItem);

        throw metamacWebException;
    }

    private void throwExternalItemNotExternallyPublishedException(String externalItemName) throws MetamacWebException {
        MetamacWebExceptionItem exceptionItem = new MetamacWebExceptionItem(WebMessageExceptionsConstants.RESOURCE_ERROR_EXTERNAL_ITEM_NOT_EXTERNALLY_PUBLISHED,
                "The external item has not been externally published", externalItemName);
        MetamacWebException metamacWebException = new MetamacWebException();
        metamacWebException.getWebExceptionItems().add(exceptionItem);

        throw metamacWebException;
    }
}
