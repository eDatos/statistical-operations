package org.siemac.metamac.statistical.operations.web.server.utils;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.v1_0.domain.ListBase;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Categories;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategorySchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Codelists;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Codes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.DataProviderSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.DataProviders;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationUnitSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationUnits;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ResourceInternal;
import org.siemac.metamac.web.common.server.utils.DtoUtils;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

public class ExternalItemUtils extends org.siemac.metamac.web.common.client.utils.ExternalItemUtils {

    //
    // CATEGORIES
    //

    // Category Schemes

    public static ExternalItemsResult getCategorySchemesAsExternalItemsResult(CategorySchemes categorySchemes) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(categorySchemes);
        result.setExternalItemDtos(getCategorySchemesAsExternalItemDtos(categorySchemes.getCategorySchemes()));
        return result;
    }

    public static ExternalItemDto getCategorySchemeAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.CATEGORY_SCHEME);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getCategorySchemesAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getCategorySchemeAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    // Categories

    public static ExternalItemsResult getCategoriesAsExternalItemsResult(Categories categories) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(categories);
        result.setExternalItemDtos(getCategoriesAsExternalItemDtos(categories.getCategories()));
        return result;
    }

    public static ExternalItemDto getCategoryAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.CATEGORY);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getCategoriesAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getCategoryAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    //
    // CODES
    //

    // Codelist

    public static ExternalItemsResult getCodelistsAsExternalItemsResult(Codelists codelists) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(codelists);
        result.setExternalItemDtos(getCodelistsAsExternalItemDtos(codelists.getCodelists()));
        return result;
    }

    public static ExternalItemDto getCodelistAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.CODELIST);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getCodelistsAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getCodelistAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    // Codes

    public static ExternalItemsResult getCodesAsExternalItemsResult(Codes codes) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(codes);
        result.setExternalItemDtos(getCodesAsExternalItemDtos(codes.getCodes()));
        return result;
    }

    public static ExternalItemDto getCodeAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.CODE);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getCodesAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getCodeAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    //
    // ORGANISATIONS
    //

    // Organisation Unit Schemes

    public static ExternalItemsResult getOrganisationUnitSchemesAsExternalItemsResult(OrganisationUnitSchemes organisationUnitSchemes) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(organisationUnitSchemes);
        result.setExternalItemDtos(getOrganisationUnitSchemesAsExternalItemDtos(organisationUnitSchemes.getOrganisationUnitSchemes()));
        return result;
    }

    public static ExternalItemDto getOrganisationUnitSchemeAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getOrganisationUnitSchemesAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getOrganisationUnitSchemeAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    // Organisation units

    public static ExternalItemsResult getOrganisationUnitsAsExternalItemsResult(OrganisationUnits organisationUnits) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(organisationUnits);
        result.setExternalItemDtos(getOrganisationUnitsAsExternalItemDtos(organisationUnits.getOrganisationUnits()));
        return result;
    }

    public static ExternalItemDto getOrganisationUnitAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.ORGANISATION_UNIT);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getOrganisationUnitsAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getOrganisationUnitAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    // Data Provider Schemes

    public static ExternalItemsResult getDataProviderSchemesAsExternalItemsResult(DataProviderSchemes dataProviderSchemes) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(dataProviderSchemes);
        result.setExternalItemDtos(getDataProviderSchemesAsExternalItemDtos(dataProviderSchemes.getDataProviderSchemes()));
        return result;
    }

    public static ExternalItemDto getDataProviderSchemeAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getDataProviderSchemesAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getDataProviderSchemeAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    // Data providers

    public static ExternalItemsResult getDataProvidersAsExternalItemsResult(DataProviders dataProviders) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(dataProviders);
        result.setExternalItemDtos(getDataProvidersAsExternalItemDtos(dataProviders.getDataProviders()));
        return result;
    }

    public static ExternalItemDto getDataProviderAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.DATA_PROVIDER);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getDataProvidersAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getDataProviderAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    //
    // COMMON METHODS
    //

    private static ExternalItemsResult getListBaseAsExternalItemsResult(ListBase listBase) {
        ExternalItemsResult result = new ExternalItemsResult();
        if (listBase != null) {
            result.setFirstResult(listBase.getOffset() != null ? listBase.getOffset().intValue() : 0);
            result.setTotalResults(listBase.getOffset() != null ? listBase.getTotal().intValue() : 0);
        }
        return result;
    }

    private static ExternalItemDto getResourceInternalAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = new ExternalItemDto();
        externalItemDto.setCode(resourceInternal.getId());
        externalItemDto.setUri(resourceInternal.getSelfLink().getHref());
        externalItemDto.setUrn(resourceInternal.getUrn());
        externalItemDto.setTitle(DtoUtils.getInternationalStringDtoFromInternationalString(resourceInternal.getTitle()));
        externalItemDto.setManagementAppUrl(resourceInternal.getManagementAppLink());
        return externalItemDto;
    }
}
